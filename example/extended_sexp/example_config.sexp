;; (
;;  (systems ("trading-system1" "trading-system2" "trading-system3" "trading-system4"))
;;  (counterparty_numbers (1 2 3 4 5 6 10))
;;  )
(
 (systems (trading-system{1..10}))
 (counterparty_numbers ({1..6,10,13,100..1000..100}))
 (chars ({A..Z}))
 (dates (2013-06-{10..20}))
 )
 
         
