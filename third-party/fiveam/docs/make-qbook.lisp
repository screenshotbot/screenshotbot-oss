(asdf:oos 'asdf:load-op :FiveAM)
(asdf:oos 'asdf:load-op :qbook)

(asdf:oos 'qbook:publish-op :FiveAM
          :generator (make-instance 'qbook:html-generator
                                    :title "FiveAM"
                                    :output-directory
                                    (merge-pathnames
                                        (make-pathname :directory '(:relative "docs" "html"))
                                        (asdf:component-pathname (asdf:find-system :FiveAM)))))

          

