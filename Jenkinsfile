
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

pipeline {
    agent {
        label 'master';
    }

    stages {

        stage ("Checkout on primary") {
            steps {
                doCheckout()
            }
        }

        stage("Run tests in parallel") {


            parallel {
                stage("run on LIspworks") {
                    agent {
                        label 'master'
                    }
                    steps {
                        doCheckout()
                        sh "make clean-sys-index"
                        sh "make test-lw"
                        sh "make selenium-tests"
                        sh "make web-bin"
                    }
                }

                stage ("test on SBCL") {
                    steps {
                        doCheckout()
                        sh "make update-quicklisp"
                        sh "make clean-sys-index"
                        sh "make test-sb"
                    }
                }

                stage("run on CCL") {
                    agent any
                    steps {
                        doCheckout()
                        sh "make clean-sys-index"
                        sh "make test-ccl"
                    }
                }

                stage ("Copybara") {
                    steps {
                        sh "make conditional-copybara"
                    }
                }
            }
        }


    }
    post {
        success {
            sh "make update-harbormaster-pass"
        }

        failure {
            sh "make update-harbormaster-fail"
        }
    }
}
