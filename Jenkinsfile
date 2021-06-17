def doCheckout () {
    checkout([
        $class: 'GitSCM',
        branches: [[name: env.GIT_SHA ]],
        userRemoteConfigs: [[credentialsId: 'jenkins-root', url: 'https://github.com/screenshotbot/screenshotbot-oss.git']]
    ]
    )
}


def cleanRepo () {
    common.doCheckout()
    sh "git clean -ffd"
    sh "make clean-sys-index"
    sh "git status"
}

pipeline {
    stage ('Run tests on CCL'){
        steps {
            cleanRepo()
            sh "make test-ccl"
        }
    }
}
