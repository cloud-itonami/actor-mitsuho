(require '[clojure.test :as t]
         'mitsuho.cells.test-cells
         'mitsuho.methods.test-agent
         'mitsuho.methods.test-charter-gates
         'mitsuho.murakumo-test
         'mitsuho.repository-contract-test)
(let [r (t/run-tests 'mitsuho.cells.test-cells 'mitsuho.methods.test-agent
                     'mitsuho.methods.test-charter-gates 'mitsuho.murakumo-test
                     'mitsuho.repository-contract-test)]
  (System/exit (if (zero? (+ (:fail r) (:error r))) 0 1)))
