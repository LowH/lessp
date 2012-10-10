lessp  -  Generic order predicate
=================================

Compare anything
----------------

(lessp 'a 'b)
=> T

(lessp 'a "a")
=> T

(lessp '(a b 1) '(a b 2))
=> T

Your own order
--------------

(defstruct fruit weight price)

(defmethod lessp ((a fruit) (b fruit))
  (lessp (/ (fruit-price a) (fruit-weight a))
  	 (/ (fruit-price b) (fruit-weight b))))

(let ((apple (make-fruit :price 0.3 :weight 100))
      (orange (make-fruit) :price 4.5 :weight 1000))
  (lessp apple orange))

=> T

Contribute
----------

This project welcomes LESSP methods for types that have an [atomic type
specifier][1], with an emphasis on speed and functional correctness.

[1]: http://www.lispworks.com/documentation/lw51/CLHS/Body/04_bc.htm

To submit contributions, just fork the project and submit a pull request.
