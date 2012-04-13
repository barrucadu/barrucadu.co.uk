$(document).ready (function ()
  {
    $("#hidden").hide ();

    $("#show").click(function ()
      {
        $("#hidden").slideToggle (400);
        return false;
      });
  });
