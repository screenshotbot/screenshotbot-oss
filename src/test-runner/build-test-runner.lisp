
(ql:quickload :test-runner)

(test-runner:init)

(hcl:save-image "build/t"
                :console t
                :restart-function #'test-runner:image-main
                :multiprocessing t
                :environment nil)
