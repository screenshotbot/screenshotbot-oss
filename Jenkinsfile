
def doCheckout () {
    checkout([
        $class: 'GitSCM',
        branches: [[name: env.GIT_SHA ]],
        doGenerateSubmoduleConfigurations: false,
        extensions: [[$class: 'SubmoduleOption',
                      disableSubmodules:false,
                      parentCredentials: true,
                      recursiveSubmodules:true,
                      reference:'',
                      trackingSubmodules:false]],
        submoduleCfg: [],
        userRemoteConfigs: [[credentialsId: 'jenkins', url: 'https://github.com/screenshotbot/screenshotbot-oss.git']]
    ]
    )
}

def cleanRepo () {
    sh "git clean -ffd"
    sh "make clean-sys-index"

    // Some debugging information to make sure
    // we're doing this pipeline correctly.
    sh "git status"
}

pipeline {
    agent {
        label 'master';
    }

    stages {

        stage ("Checkout on primary") {
            steps {
                doCheckout()
                //stash 'source'
            }
        }

        stage("Run tests in parallel") {


            parallel {
                stage("run on Lispworks") {
                    agent {
                        label 'master'
                    }
                    steps {
                        //unstash 'source'
                        cleanRepo()
                        sh "make test-lw"
                        sh "test -f src/screenshotbot.oss.tests.asd || make test-store"
                        sh "test -f src/screenshotbot.oss.tests.asd || make selenium-tests"
                        sh "test -f src/screenshotbot.oss.tests.asd || make web-bin"
                    }
                }

                stage ("test on SBCL") {
                    agent any
                    steps {
                        //unstash 'source'

                        cleanRepo()
                        sh "make update-quicklisp"
                        sh "make test-sb"
                    }
                }

                stage("run on CCL") {
                    agent any
                    steps {
                        //unstash 'source'
                        cleanRepo()
                        sh "make test-ccl"
                    }
                }

                stage ("Copybara") {
                    steps {
                        sh "test -f src/screenshotbot.oss.tests.asd || make conditional-copybara"
                    }
                }
            }
        }


    }
    post {
        success {
            sh "test -f src/screenshotbot.oss.tests.asd || make update-harbormaster-pass"
        }

        failure {
            sh "test -f src/screenshotbot.oss.tests.asd || make update-harbormaster-fail"
        }
    }
}
