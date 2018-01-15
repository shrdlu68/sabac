;; A Simple Adaptive Binary Arithmetic Coder

(defun encode-arithmetic (bv)
  "The encoder: requires only the bit vector to be encoded as an argument"
  (let* ((len (length bv))
	 (output (make-array len :element-type 'bit :adjustable t :fill-pointer 0)))
    (loop with low = 0
	  with high = 1
	  with follow-on = 0
	  for bit-index from 0 below len
	  for bit = (aref bv bit-index)
	  for range = (- high low)
	  with on-count = 1
	  with off-count = 1
	  with encoded = 2
	  for off-probability = (/ off-count encoded)
	  for on-probability =  (/ on-count encoded)
	  for on-interval = (cons low (+ low (* range on-probability)))
	  for off-interval = (cons (cdr on-interval)
				   (+ (cdr on-interval) (* range off-probability)))
	  do (cond ((plusp bit)
		    (setf low (car on-interval))
		    (setf high (cdr on-interval))
		    (incf on-count))
		   (t
		    (setf low (car off-interval))
		    (setf high (cdr off-interval))
		    (incf off-count)))
	     (incf encoded)
	     (setf range (- high low))
	     (loop while (or (<= high 1/2)
			     (< 1/2 low)
			     (and (<= 1/4 low)
				  (< high 3/4)))
		   do
		      (cond ((<= high 1/2)
			     (vector-push-extend 0 output)
			     (when (plusp follow-on)
			       (loop repeat follow-on do (vector-push-extend 1 output))
			       (setf follow-on 0))
			     (setf low (* low 2))
			     (setf high (+ low (* range 2)))
			     (setf range (- high low)))
			    ((< 1/2 low)
			     (vector-push-extend 1 output)
			     (when (plusp follow-on)
			       (loop repeat follow-on do (vector-push-extend 0 output))
			       (setf follow-on 0))
			     (setf high (- 1 (* (- 1 high) 2)))
			     (setf low (- high (* 2 range)))
			     (setf range (- high low)))
			    (t
			     (incf follow-on)
			     (setf low (- 1/2 (* (- 1/2 low) 2)))
			     (setf high (+ 1/2 (* (- high 1/2) 2)))
			     (setf range (- high low)))))
	  finally (vector-push-extend 1 output))
    (values (subseq output 0 (fill-pointer output)) len)))

(defun decode-arithmetic (bv &optional (len 256))
  "The decoder: requires the output of the encoder and the length of the output"
  (let* ((output (make-array len :element-type 'bit))
	 (final-interval (loop for n = 1/2 then (* n 1/2)
			       for bit across bv
			       when (plusp bit)
				 sum n)))
    (loop with low = 0
	  with high = 1
	  with on-count = 1
	  with off-count = 1
	  with encoded = 2
	  for off-probability = (/ off-count encoded)
	  for on-probability =  (/ on-count encoded)
	  for range = (- high low)
	  for on-interval = (cons low (+ low (* range on-probability)))
	  for off-interval = (cons (cdr on-interval)
				   (+ (cdr on-interval) (* range off-probability)))
	  for output-index from 0 below len
	  do (cond ((< final-interval (cdr on-interval))
		    (setf (aref output output-index) 1)
		    (setf low (car on-interval))
		    (setf high (cdr on-interval))
		    (incf on-count))
		   ((<= (car off-interval) final-interval)
		    (setf (aref output output-index) 0)
		    (setf low (car off-interval))
		    (setf high (cdr off-interval))
		    (incf off-count)))
	     (incf encoded))
    output))

(deftype octet ()
  '(unsigned-byte 8))

(defun make-octet-vector (length)
  (make-array length :element-type 'octet))

(defun read-octets (path &optional n)
  "Read n octets from path, or the entire file"
  (with-open-file (fd path :direction :input :element-type 'octet)
    (let* ((len (or n (file-length fd)))
	   (ov (make-octet-vector len)))
      (loop for index from 0 below len do
	(setf (aref ov index) (read-byte fd)))
      ov)))

(defun ov-to-bv (ov)
  "Simple conversion of octet vectors to bit vectors"
  (let ((bv (make-array (* (length ov) 8) :element-type 'bit)))
    (loop for index from 0 below (length ov)
	  for octet = (aref ov index)
	  for bv-index from 0 by 8 do
	    (loop for i from 0 below 8 do
	      (setf (aref bv (+ bv-index i)) (ldb (byte 1 i) octet))))
    bv))

(defun int-to-bv (int len)
  (let ((bv (make-array len :element-type 'bit)))
    (loop for index from 0 below len
	  for bv-index downfrom (1- (length bv)) do
      (setf (aref bv bv-index) (ldb (byte 1 index) int)))
    bv))

(defun bv-to-int (bv &key (start 0) (end (length bv)))
  (let ((int 0))
    (loop for bv-index downfrom (1- end) to start
	  for index from 0 do
	    (setf (ldb (byte 1 index) int) (aref bv bv-index)))
    int))

(defun percentage (m n)
  (* (float (/ m n)) 100))

(defun test1 (i)
  "Test the encoder & decoder on all bitsrings of length i"
  (percentage (loop for n from 0 to (1- (expt 2 i))
		    for bv = (int-to-bv n i)
		    for ae = (encode-arithmetic bv)
		    for ad = (decode-arithmetic ae i)
		    counting (equal bv ad))
	      (expt 2 i)))

(defun test2 (len iterations)
  "Tests the encoder & decoder using len octets from /dev/urandom iteration times"
  (percentage (loop repeat iterations
		    for ov = (read-octets "/dev/urandom" len)
		    for bv = (ov-to-bv ov)
		    counting (multiple-value-bind (out len) (encode-arithmetic bv)
			       (equal (decode-arithmetic out len) bv)))
	      iterations))
