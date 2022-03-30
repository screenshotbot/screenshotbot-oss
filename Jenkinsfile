def doCheckout () {
    checkout([
        $class: 'GitSCM',
        branches: [[name: env.GIT_SHA ]],
        userRemoteConfigs: [[credentialsId: 'jenkins-root', url: 'https://github.com/screenshotbot/screenshotbot-oss.git']]
    ]
    )
}


def cleanRepo () {
    sh "git clean -ffd"
    sh "make clean-sys-index"
    sh "git status"
}

pipeline {
    agent any

    stages {
        stage ('Run tests on CCL'){
            steps {
                doCheckout()
                cleanRepo()
                sh "make test-ccl"
            }
        }
    }
}
