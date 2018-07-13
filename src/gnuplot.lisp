(in-package :losh.gnuplot)

(defun gnuplot-args% (&rest args)
  (mapcan (lambda (arg) (list "-e" arg))
          (remove nil args)))

(defun gnuplot-args (&key
                     (output :wxt)
                     (filename "plot.png")
                     (style :lines)
                     (size-x 1200)
                     (size-y 800)
                     (label-x)
                     (label-y)
                     (line-title 'data)
                     (line-width 4)
                     (smooth nil)
                     (axis-x nil)
                     (axis-y nil)
                     (min-x nil)
                     (max-x nil)
                     (min-y nil)
                     (max-y nil)
                     (tics-x nil)
                     (graph-title)
                     (logscale-x nil)
                     (logscale-y nil)
                     (box-width nil)
                     &allow-other-keys)
  "Return the formatted command line arguments for the given gnuplot arguments.

  You shouldn't call this function directly — it's exposed just so you can see
  the list of possible gnuplot arguments all in one place.

  "
  (flet ((esc (string) (remove #\' (aesthetic-string string)))
         (f (&rest args) (apply #'format nil (substitute "" nil args))))
    (gnuplot-args%
      (ccase output
        ((:x :x11) (f "set terminal x11 persist"))
        (:qt (f "set terminal qt persist"))
        (:wxt (f "set terminal wxt persist"))
        (:png
         (f "set terminal pngcairo dashed size ~D,~D font \"Lucida Grande,20\""
            size-x size-y)
         (f "set output '~A'" (esc filename))))
      (f "set border linewidth 1")
      (f "set style line 10 dashtype 2 linewidth 3 linecolor \"#666666\"")
      (when axis-x (f "set xzeroaxis linestyle 10"))
      (when tics-x (f "set xtics ~A" tics-x))
      (when axis-y (f "set yzeroaxis linestyle 10"))
      (when box-width (f "set boxwidth ~A" box-width))
      (when graph-title (f "set title '~A'" (esc graph-title)))
      (when label-x (f "set xlabel '~A'" (esc label-x)))
      (when label-y (f "set ylabel '~A'" (esc label-y)))
      (when logscale-x (f "set logscale x"))
      (when logscale-y (f "set logscale y"))
      (f "set xrange [~A:~A]" min-x max-x)
      (f "set yrange [~A:~A]" min-y max-y)
      (f "plot '-' using 1:2 title '~A' with ~(~A~) linewidth ~D ~A"
         (esc line-title) style line-width
         (when smooth (f "smooth ~(~A~)" smooth))))))


(defun gnuplot (data
                &rest args
                &key
                (x #'car)
                (y #'cdr)
                (spew-output nil)
                &allow-other-keys)
  "Plot `data` to `filename` with gnuplot.

  This will (silently) quickload the `external-program` system to handle the
  communication with gnuplot.

  `data` should be a sequence of data points to plot.

  `x` should be a function to pull the x-values from each item in data.

  `y` should be a function to pull the y-values from each item in data.

  See the docstring of `gnuplot-args` for other keyword arguments.

  "
  (uiop/package:symbol-call :ql :quickload 'external-program :silent t)
  (let* ((process (uiop/package:symbol-call
                    :external-program :start
                    "gnuplot"
                    (apply #'gnuplot-args args)
                    :input :stream
                    :output (if spew-output *standard-output* nil)))
         (in (uiop/package:symbol-call
               :external-program :process-input-stream
               process)))
    (unwind-protect
        (progn
          (iterate (for item :in-whatever data)
                   (format in "~F ~F~%" (funcall x item) (funcall y item)))
          (finish-output in))
      (close in))
    process))

(defun gnuplot-function (function
                         &rest args
                         &key
                         (start 0.0)
                         (end 1.0)
                         (step 0.1)
                         (include-end nil)
                         &allow-other-keys)
  "Plot `function` over [`start`, `end`) by `step` with gnuplot.

  If `include-end` is `t` the `end` value will also be plotted.

  See the docstring of `gnuplot-args` for other keyword arguments.

  "
  (let* ((x (range start end :step step))
         (x (append x
                    (when (and include-end
                               (/= (car (last x)) end))
                      (list end))))
         (y (mapcar function x))
         (data (mapcar #'cons x y)))
    (apply #'gnuplot data args)))


(defmacro gnuplot-expr (expr &rest args)
  "Plot `expr` (an expression involving `x`) with gnuplot.

  See the docstring of `gnuplot-args` for other keyword arguments.

  "
  `(gnuplot-function (lambda (x) ,expr)
    :line-title ',expr
    ,@args))


(defun gnuplot-histogram (data
                          &rest args
                          &key (bin-width 1)
                          &allow-other-keys)
  "Plot `data` as a histogram with gnuplot.

  `bin-width` should be the desired width of the bins.  The bins will be
  centered on multiples of this number, and data will be rounded to the nearest
  bin.

  "
  (-<> data
    (mapcar (lambda (y)
              (* bin-width (round y bin-width)))
            <>)
    frequencies
    hash-table-alist
    (apply #'gnuplot <>
           :style :boxes
           :min-y 0
           :line-width 1
           :box-width (* bin-width 1.0)
           args)))


