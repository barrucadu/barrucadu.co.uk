pipeline {
  agent any
  stages {
    stage('build') {
      steps {
        sh '''stack build
stack exec hakyll build'''
      }
    }
    stage('deploy') {
      steps {
        sh '''WEB_DIR=/var/lib/containers/barrucadu/srv/http/www
cv=""

if [[ -e $WEB_DIR/cv.pdf ]]; then
  cv=`mktemp`
  mv $WEB_DIR/cv.pdf $cv
fi

rm -r $WEB_DIR/*
cp -a _site/* $WEB_DIR
m4 nginx.conf.m4 > $WEB_DIR/../www.conf

if [[ ! -z "$cv" ]]; then
  mv $cv $WEB_DIR/cv.pdf
fi'''
      }
    }
  }
}
