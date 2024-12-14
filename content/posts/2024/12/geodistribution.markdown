---
title: Geodistributing my Website (with GeoDNS and NixOS)
template: feed.html
feed:
  image: "posts/amsterdam_harbor_scene_2011.3.1.jpg"
  image_alt: "Amsterdam Harbor Scene (c. 1654/1655, Reinier Nooms)"
  published: 2024-12-14
  summary: "Inspired by a few blog posts I read recently, I set up a mirror of my website in the US and configured GeoDNS to route American visitors there.  Covers: NixOS on Hetzner VMs, ACME DNS-01 (NixOS & AWS IAM config), Route53 GeoDNS, and NixOS distributed builds."
---

<figure>
  <img src="{{ feed.image }}" alt="{{ feed.image_alt }}">
  <figcaption>{{ feed.image_alt }}</figcaption>
</figure>

<h2 style="display: none">Introduction</h2>

I derive a lot of enjoyment from fiddling around with my servers (it's more fun
than it sounds!) so when I saw a blog post about [setting up a tiny CDN with
infrastructure-as-code][tinycdn] my first thought was "I bet that would be
pretty easy to replicate with NixOS."

A day later I had `yuggoth.barrucadu.co.uk` up and running: a server in Virginia
with a mirror of this website, [my weeknotes][], [bookdb][], and [bookmarks][];
with traffic from the Americas routed via GeoDNS; and with TLS certs from
LetsEncrypt provisioned by DNS challenge.

Is it over-engineered?  Yes.  Was it necessary?  No.  Did I learn some new
things along the way?  Yes!

This post goes into some of those things.

## Context: How I run things

I'm a heavy believer in infrastructure-as-code and configuration-as-code.  In
this little project we're going to need to touch:

- **Servers:** for which I use [NixOS][] and [Hetzner][]
- **DNS:** for which I use [OctoDNS][] and [AWS Route53][]
- **AWS IAM:** for which I use [Terraform][]
- **Continuous Deployment:** for which I use [Concourse][]

### Software

[My NixOS configuration is on GitHub][nixfiles], and is fairly extensive.  A
detailed introduction to NixOS is beyond the scope of this article, but suffice
it to say that NixOS is a Linux distribution that takes configuration-as-code
really seriously.

I take it to the extreme by configuring my servers to wipe `/` on boot and
rebuild the system from the defined configuration, with only a small amount of
explicitly chosen state preserved across reboots (like, say, SSH private keys,
or the contents of `/srv/http`).

This is helpful here because it means we can be absolutely certain of what we
need to replicate to the new server to ensure we have a duplicate of the
existing one.  There is no possibility of any forgotten state or manual
configuration lingering in a dusty corner of the filesystem

### Infrastructure

[My "ops" configuration is on GitHub][ops], covering AWS, DNS, and a few other
small things.  It's fairly standard I think, I don't do anything crazy here.

I *don't* provision servers through Terraform, I bring those up manually.  I've
had several servers over multiple providers over the years, not all of which
have had APIs or which Terraform has supported, so I've just not got around to
defining the servers I do have at the moment in code.

### Deployments

Each repository has its own Concourse configuration, see [the barrucadu.co.uk
pipeline][] for a representative example.

## Installing NixOS on Hetzner Cloud

I use the Hetzner web UI to create servers.  Some points of note:

1. **Location:** they have datacentres in Europe, the US, and Singapore.  My
   main server, `carcosa.barrucadu.co.uk`, is in Germany, so I picked one of the
   US locations.

2. **Image:** Hetzner doesn't have a prebuilt NixOS image, so we'll be
   reinstalling the OS.  Leave this on the default (Ubuntu, at the time of
   writing).

3. **Type:** I went for an x86 machine with a shared vCPU.  I don't need the
   power of a dedicated vCPU, and I wanted the same architecture as my current
   server so I wouldn't have any potential surprises with package availability.

4. **Networking:** IPv6 is free, IPv4 is not.  But is IPv4 ever going to die?
   We need both types.

5. **SSH Keys:** We're going to reinstall the OS, so no need to provide a key
   here.

6. **Volumes:** The disk space the instance type comes with is more than enough
   for my needs, so I didn't add any extra volumes.

7. **Firewalls:** I rely on the NixOS firewall module, which has fairly sensible
   defaults, so I don't use Hetzner's firewall.  Maybe I would if I got more
   traffic.

8. **Backups:** I manage my own backups, and as a mirror this new server won't
   have anything worth backing up anyway.

9. **Placement Groups, Labels, and Cloud Config:** None of this is relevant here.

10. **Name:** This is, obviously, the most important decision.  I'm sure it's
    apparent to all why I chose `yuggoth` for this one (and `carcosa` for my
    main server in Europe).

Then click **Create & Buy now** and briefly think about the extra several euros
a month you'll be spending on a pointless vanity project.

Next up we want to replace the standard OS with NixOS.  From your new server's
status / management pages you can mount an ISO.  At the time of writing, it has
NixOS 24.05 available.

1. Mount the ISO
2. Reboot the server
3. Open the web console
4. Follow [the installation section of the NixOS manual][]

The *final thing* is that you need to manually configure IPv6.  At least,
according to an article I followed in a cargo-cultish fashion years ago, I don't
know if it's still required.  But even so, just in case, configure the prefix
and default gateway in your `configuration.nix`:

```nix
networking.interfaces.enp1s0 = {
  ipv6.addresses = [{ address = "..."; prefixLength = 64; }];
};
networking.defaultGateway6 = { address = "fe80::1"; interface = "enp1s0"; };
```

You can get the `address` from the Hetzner web UI.

<div class="box" markdown="1">
<div><img src="konata/point.webp" alt=""></div>
<div class="box-text" markdown="1">
Because I have standard practices for how I partition and configure my machines,
[I have a runbook][] (and a helper script) for this whole process, which also
covers setting up secrets.
</div>
</div>

## Provisioning HTTPS certificates

LetsEncrypt is great, but obviously the HTTP challenge (putting a file at a
known location) isn't going to work here, because I'll have two servers hosting
the same domains.  I'd somehow need to ensure that LetsEncrypt would hit the US
server for the US HTTP challenge and the EU server for the EU HTTP challenge.
That might just work, due to the power of GeoDNS (if LetsEncrypt is doing the
same and has a US instance), but it sounds incredibly fragile.

So we want to use [the DNS challenge][].

To get this working I basically followed [this blog post on how someone set up
Caddy with SSL in their homelab][homelab].

### Route53 and IAM

My DNS records are hosted in [AWS Route53][].  While I normally manage them with
[OctoDNS][], [LEGO][] (the LetsEncrypt client used by the NixOS `security.acme`
module) will need to be able to modify records in the same zones too: the DNS-01
challenge records.

I didn't want to give it full access to all my records, just limited access to
the ones it needed.  Fortunately, the LEGO documentation [gives us a minimal IAM
policy][].  So this was just a matter of creating an IAM user and giving it the
policies needed to access my zones:

```terraform
locals {
  hosted_zones = {
    barrucadu_co_uk = {
      domain = "barrucadu.co.uk"
      zone_id = "..."
    }
    # ...
  }
}

resource "aws_iam_user" "acme_dns_challenge" {
  name = "acme-dns-challenge"
}

resource "aws_iam_user_policy_attachment" "acme_dns_challenge" {
  for_each = local.hosted_zones

  user       = aws_iam_user.acme_dns_challenge.name
  policy_arn = aws_iam_policy.acme_dns_challenge[each.key].arn
}

resource "aws_iam_policy" "acme_dns_challenge" {
  for_each = local.hosted_zones

  name        = "acme-dns-challenge-${each.value.domain}"
  description = "Restricted access to the zone for ${each.value.domain}."
  policy      = data.aws_iam_policy_document.acme_dns_challenge[each.key].json
}

data "aws_iam_policy_document" "acme_dns_challenge" {
  for_each = local.hosted_zones

  statement {
    actions = [
      "route53:GetChange",
    ]

    resources = [
      "arn:aws:route53:::change/*",
    ]
  }
  statement {
    actions = [
      "route53:ListHostedZonesByName",
    ]

    resources = [
      "*",
    ]
  }
  statement {
    actions = [
      "route53:ListResourceRecordSets",
    ]

    resources = [
      "arn:aws:route53:::hostedzone/${each.value.zone_id}",
    ]
  }
  statement {
    actions = [
      "route53:ChangeResourceRecordSets",
    ]

    resources = [
      "arn:aws:route53:::hostedzone/${each.value.zone_id}",
    ]

    condition {
      test = "ForAllValues:StringEquals"
      variable = "route53:ChangeResourceRecordSetsNormalizedRecordNames"
      values = ["_acme-challenge.${each.value.domain}"]
    }

    condition {
      test = "ForAllValues:StringEquals"
      variable = "route53:ChangeResourceRecordSetsRecordTypes"
      values = ["TXT"]
    }
  }
}
```

If you're not familiar with Terraform, this is creating:

- An IAM user called `acme-dns-challenge`
- One IAM policy for each entry in `locals.hosted_zones`
- One IAM Policy Attachment between the user and each of those policies

Once this was applied, I used the AWS console to generate an access token and
added the credentials to my NixOS secrets (for which I use [sops-nix][]).

### Requesting the certificates

The NixOS `security.acme` module provides a nice abstraction for declaring the
certificates you need, and handles the systemd timers, file permissions, and
webserver-reloading for you.  Here's what it looks like for one domain:

```nix
security.acme = {
  acceptTerms = true;
  defaults = {
    email = "mike@barrucadu.co.uk";
    dnsProvider = "route53";
    dnsPropagationCheck = true;
    environmentFile = config.sops.secrets."services/acme/env".path;
    reloadServices = [ "caddy" ];
  };
  certs."barrucadu.co.uk" = {
    group = config.services.caddy.group;
    domain = "barrucadu.co.uk";
    extraDomainNames = [ "*.barrucadu.co.uk" ];
    postRun = ''
      mkdir -p /persist/var/lib/acme || true
      rm -r /persist/var/lib/acme/barrucadu.co.uk || true
      cp -a /var/lib/acme/barrucadu.co.uk /persist/var/lib/acme/barrucadu.co.uk
    '';
  };
  # ...
};

sops.secrets."services/acme/env" = { };
```

Here `sops.secrets."services/acme/env"` corresponds to an encrypted secret.
sops-nix handles decrypting secrets to secure locations (only readable by the
configured owner) for us.  The secret in this case is an environment file of the
form:

```sh
AWS_ACCESS_KEY_ID=...
AWS_SECRET_ACCESS_KEY=...
```

That bit around copying files to `/persist/...` is to move the certificates out
of `/var/lib`, as I have set that up to get wiped on boot.

### Placeholder TXT records

After setting the previous two things up I hit an unexpected snag: LEGO doesn't
like wildcard `CNAME` records.

You see, I have records like `*.barrucadu.co.uk CNAME barrucadu.co.uk`, which
made LEGO think that the challenge record, `_acme-challenge.barrucadu.co.uk`,
already existed, which confused it.

So I had to create empty TXT records, to override the wildcard `CNAME`s, for the
all the domains I wanted to mirror:

- `_acme-challenge.barrucadu.co.uk`
- `_acme-challenge.barrucadu.com`
- `_acme-challenge.barrucadu.dev`
- `_acme-challenge.barrucadu.uk`

These have a value of "" (the actual value is irrelevant) and a TTL of 10.  In
OctoDNS-speak, that looks like this:

```yaml
"_acme-challenge":
  ttl: 10
  type: "TXT"
  values:
    - ""
```

### Using the certificates

The final piece of the puzzle is configuring [Caddy][], my webserver of choice,
to use these certs.  This is accomplished by adding a little boilerplate to the
config of each virtualhost:

```caddyfile
barrucadu.co.uk {
  tls /persist/var/lib/acme/barrucadu.co.uk/cert.pem /persist/var/lib/acme/barrucadu.co.uk/key.pem {
    protocols tls1.3
  }

  ...
}
```

<div class="box" markdown="1">
<div><img src="konata/point.webp" alt=""></div>
<div class="box-text" markdown="1">
I actually have all this NixOS config abstracted, so I'm not typing everything
out verbatim for every domain, but for this article I de-abstracted the examples
to a single domain to make what's going on clear.

Go see [the actual configuration][website-mirror] if you're interested in what
it really looks like.
</div>
</div>

## Deployments!

I didn't do anything fancy here, I just configured everything that I wanted to
mirror to deploy to the new server as well as the existing one.

My static websites are deployed via `rsync` on push to GitHub.  That's not very
interesting.

[bookdb][] and [bookmarks][] are stateful services backed up by an
[Elasticsearch][] instance.  That's a bit more interesting.

### bookdb & bookmarks remote sync

Fortunately keeping bookdb & bookmarks in sync across multiple hosts is fairly
simple as there's only one writable instance of each: on my home server, only
accessible to my LAN.

So when I first wanted to make public mirrors of them on `carcosa` years ago, I
had to implement a push-based synchronisation method, and I went for the most
straightforward one imaginable: I take a backup on `nyarlathotep` (my home
server), copy it over to `carcosa` via SSH, and restore it.

This has the nice benefit of testing my backup & restore process every 45
minutes, so I'd know very quickly if I broke it.

So while the mechanics of syncing bookdb and bookmarks were not new, I did have
to [refactor my NixOS config a bit][] to make it able to push to multiple
targets.

## Duplicating the webserver configuration

Let's briefly review where we are:

- We have `yuggoth`, a new VPS in the US
- We have both `carcosa` and `yuggoth` provisioning HTTPS certs for the same
  domains via DNS challenge
- We have all the website files and state pushed to both

Now we need to duplicate the webserver config.  Fortunately, as NixOS is the
ultimate incarnation of configuration-as-code, *this is trivial!*

Here's what I did:

1. Move all the config I wanted to duplicate into a new file.  This was the
   `security.acme` stuff, a subset of the caddy virtualhosts, some bookdb and
   bookmarks stuff, and a few other bits and bobs ([see the full module if
   you're curious][website-mirror]).

2. Add that new file to the `imports` list for both `carcosa` and `yuggoth`

3. Apply the configuration.

Done!  It just works!  Both servers immediately provision certificates (if they
didn't have them already) and bring up the webserver!

NixOS handles merging configuration across files, so there's no problem at all
with having some caddy config in the new file and some in the main `carcosa`
configuration file.

<div class="box" markdown="1">
<div class="box-text" markdown="1">
There's no way I could go back to
configuration-as-editing-a-crapload-of-files-in-`etc` systems.
</div>
<div><img src="konata/flop.webp" alt=""></div>
</div>

## Enabling GeoDNS with OctoDNS and Route53

The new server is ready to serve traffic, so now we just need to set up the
GeoDNS!

Firstly, if (like me) you have a `CNAME` for `*.domain`, you need to make sure
there's explicit records for any subdomains that *aren't* going to be mirrored.
Like so:

```yaml
"misc":
  type: "CNAME"
  value: "carcosa.barrucadu.co.uk."
```

Now for the GeoDNS.  The OctoDNS documentation [does cover "dynamic records"
like this][] but I got it wrong at first as it misses out (or at least, isn't
super clear on) a crucial detail about healthchecks.

So let's see an example from [my ops repository][ops]:

```yaml
"":
  - dynamic:
      pools:
        # yuggoth
        america:
          values:
            - value: "178.156.151.57"
        # carcosa
        rest_of_world:
          values:
            - value: "116.203.34.201"
      rules:
        - geos:
            - "NA"
            - "SA"
          pool: "america"
        - pool: "rest_of_world"
    octodns:
      healthcheck:
        protocol: "TCP"
    type: "A"
    values:
      - "178.156.151.57"
      - "116.203.34.201"
```

This is defining a dynamic `A` record (the `AAAA` case is analogous) for the
apex domain (`barrucadu.co.uk` in this case, though you can't tell from this
excerpt).

The interesting parts are:

- `dynamic.pools`: this is defining sets of values.  I have defined two pools:
  `america` and `rest_of_world`

- `dynamic.rules`: this is an *unordered* list of geolocation rules.  If the
  source IP matches multiple rules, the most specific one will be used.  If the
  source IP doesn't match any rules, *no result will be returned*.  So it's very
  important to have a catch-all rule!

- `octodns.healthcheck`: this is of crucial importance.  The values in
  `dynamic.pools` are only returned *if they pass their healthcheck*.  The
  default Route53 healthcheck (other providers may differ) is to check whether
  `https://[ip]/_dns` returns a 200 response.  If it does, the IP is considered
  healthy.  If it doesn't, the IP is unhealthy and *will not be returned.*  If
  every IP in a pool is unhealthy, it falls back to the `values` list.

    A `TCP` healthcheck just checks that the remote server is up, rather than
    requiring a specific path be available.

I intiially missed the healthcheck part, so all my pools were unhealthy, so
requests were essentially going to a random servers: not what I wanted!

<div class="box" markdown="1">
<div><img src="konata/thumbsup.webp" alt=""></div>
<div class="box-text" markdown="1">
That's everything!
</div>
</div>

## Aside: Configuring distributed Nix builds

One final point.  [bookdb][] and [bookmarks][] are Rust applications, which is a
bit heavy on the CPU and RAM when compiling.  I quickly found that the smallest
instance type didn't really cope.

<div class="box" markdown="1">
<div class="box-text" markdown="1">
Should I just un-mirror them?  No, that's the coward's way out!
</div>
<div><img src="konata/ponder.webp" alt=""></div>
</div>

I decided to solve the problem by setting up [distributed builds][] which, well,
turned out to be incredibly easy.

There's two parts to it.  The builder machine (`carcosa`) needs a user the other
machine (`yuggoth`) can SSH in and run `nix` builds as:

```nix
users.extraUsers.nix-remote-builder = {
  home = "/var/lib/nix-remote-builder";
  createHome = true;
  isSystemUser = true;
  shell = pkgs.bashInteractive;
  group = "nogroup";
  openssh.authorizedKeys.keys =
    [ "ssh-ed25519 AAAAC3NzaC1lZDI1NTE5AAAAIHFzMpx7QNSAb5tCbkzMRIG62PvBZysflwwCKchFDHtY nix@yuggoth" ];
};
nix.settings.trusted-users = [ config.users.extraUsers.nix-remote-builder.name ];
```

And then the other machine (`yuggoth`) needs to be told that this builder exists
and what SSH key to use:

```nix
nix.distributedBuilds = true;
nix.buildMachines = [{
  hostName = "carcosa.barrucadu.co.uk";
  system = "x86_64-linux";
  sshUser = "nix-remote-builder";
  sshKey = config.sops.secrets."nix/build_machines/carcosa/ssh_key".path;
  publicHostKey = "c3NoLWVkMjU1MTkgQUFBQUMzTnphQzFsWkRJMU5URTVBQUFBSUlTa0x0bk11bUs3N1RYUHBSa0VCeGI1NEtZVHZMZzhHUmFOeGl6c2NoMSsgcm9vdEBjYXJjb3NhCg==";
  protocol = "ssh-ng";
  maxJobs = 8;
}];
sops.secrets."nix/build_machines/carcosa/ssh_key" = { };
```

I disabled bookdb and bookmarks on `yuggoth`, applied this configuration,
re-enabled them, applied that configuration, and lo and behold: it SSHed into
`carcosa` and completed in an instant because `carcosa` already had those build
artefacts cached.

## Inspired by

- [Building a Tiny CDN With pyinfra and Chimera Linux][tinycdn]
- [Homelab: Setting up Caddy Reverse Proxy with SSL on NixOS][homelab]

[tinycdn]: https://www.wezm.net/v2/posts/2024/tiny-cdn/
[homelab]: https://aottr.dev/posts/2024/08/homelab-setting-up-caddy-reverse-proxy-with-ssl-on-nixos/
[my weeknotes]: https://weeknotes.barrucadu.co.uk/
[bookdb]: https://bookdb.barrucadu.co.uk/
[bookmarks]: https://bookmarks.barrucadu.co.uk/
[NixOS]: https://nixos.org/
[Hetzner]: https://www.hetzner.com/
[OctoDNS]: https://github.com/octodns/octodns
[AWS Route53]: https://aws.amazon.com/route53/
[Terraform]: https://www.terraform.io/
[Concourse]: https://concourse-ci.org/
[nixfiles]: https://github.com/barrucadu/nixfiles
[ops]: https://github.com/barrucadu/ops
[the barrucadu.co.uk pipeline]: https://github.com/barrucadu/barrucadu.co.uk/blob/master/concourse/pipeline.yml
[the installation section of the NixOS manual]: https://nixos.org/manual/nixos/stable/index.html#ch-installation
[I have a runbook]: https://nixfiles.docs.barrucadu.co.uk/runbooks/set-up-a-new-host.html
[the DNS challenge]: https://letsencrypt.org/docs/challenge-types/#dns-01-challenge
[LEGO]: https://go-acme.github.io/lego/
[gives us a minimal IAM policy]: https://go-acme.github.io/lego/dns/route53/index.html
[sops-nix]: https://github.com/Mic92/sops-nix
[website-mirror]: https://github.com/barrucadu/nixfiles/blob/master/shared/host-templates/website-mirror/default.nix
[Caddy]: https://caddyserver.com/
[Elasticsearch]: https://www.elastic.co/elasticsearch
[refactor my NixOS config a bit]: https://github.com/barrucadu/nixfiles/pull/311
[does cover "dynamic records" like this]: https://github.com/octodns/octodns/blob/main/docs/dynamic_records.md
[distributed builds]: https://nix.dev/manual/nix/2.18/advanced-topics/distributed-builds
