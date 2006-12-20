;;;; test-static-analysis.lisp
;;;
;;; Unit tests for the static analysis utility functions
;;;
;;; Copyright (c) 2006 James Wright
;;; See LICENSE for full licensing details.
;;;
(in-package :jwacs-tests)

(defnote static-analysis "Tests for the simple static-analysis utility functions")

(deftest static-analysis/explicitly-terminated-p/1 :notes static-analysis
  (explicitly-terminated-p
   (test-parse "
      x = 10;
      y = 20;
      return 15;")
   '(:return :throw :break :continue :resume :suspend))
  :return)

(deftest static-analysis/explicitly-terminated-p/2 :notes static-analysis
  (explicitly-terminated-p
   (test-parse "
      x = 10;
      y = 20;
      if(x > 10)
        resume k <- 55;
      else
        suspend;")
   '(:return :throw :break :continue :resume :suspend))
  :suspend)

(deftest static-analysis/explicitly-terminated-p/3 :notes static-analysis
  (explicitly-terminated-p
   (test-parse "
      if(x)
        return;
      else
        x = 10;")
   '(:return :throw :break :continue :resume :suspend))
  nil)

(deftest static-analysis/explicitly-terminated-p/4 :notes static-analysis
  (explicitly-terminated-p
   (test-parse "
      while(true)
      {
        if(x)
          break; // Not an 'escaping' break, because it terminates the while but not the whole list
        else
          continue; // Not an escaping continue, similarly
      }
      x = 10;")
   '(:return :throw :break :continue :resume :suspend))
  nil)

(deftest static-analysis/explicitly-terminated-p/5 :notes static-analysis
  (explicitly-terminated-p
   (test-parse "
      foo:
      while(true)
      {
        if(x)
          break foo; // Not an 'escaping' break, because it terminates the while but not the whole list
        else
          continue foo; // Not an escaping continue, similarly
      }
      x = 10;")
   '(:return :throw :break :continue :resume :suspend))
  nil)

(deftest static-analysis/explicitly-terminated-p/6 :notes static-analysis
  (explicitly-terminated-p
   (test-parse "
      foo:
      while(true)
      {
        if(x)
          break foo; // Not an 'escaping' break, because it terminates the while but not the whole list
        else
          continue bar; // is an escaping continue
      }
      x = 10;")
   '(:return :throw :break :continue :resume :suspend))
  nil)

(deftest static-analysis/explicitly-terminated-p/7 :notes static-analysis
  (explicitly-terminated-p
   (test-parse "
      while(true)
      {
        if(x)
          break foo; // escaping break
        else
          continue bar; // escaping continue
      }
      x = 10;")
   '(:return :throw :break :continue :resume :suspend))
  :continue)

(deftest static-analysis/explicitly-terminated-p/8 :notes static-analysis
  (explicitly-terminated-p
   (test-parse "
      x = 50;
      // Although a human can tell that the loop's body is guaranteed to execute
      // at least once, EXPLICITLY-TERMINATED-P can't, because we're not doing
      // dataflow analysis.
      while(x < 100)
      {
        if(x)
          return 10;
        else
          return 20;
      }")
   '(:return :throw :break :continue :resume :suspend))
  nil)

(deftest static-analysis/explicitly-terminated-p/9 :notes static-analysis
  (explicitly-terminated-p
   (test-parse "
      x = 20;
      foo:
      while(true)
      {
        break;
      }")
   '(:return :throw :break :continue :resume :suspend))
  nil)

(deftest static-analysis/explicitly-terminated-p/nested-throw/1 :notes static-analysis
  (explicitly-terminated-p
   (test-parse "
      foo();
      bar();
      try
      {
        if(baz())
          return 10;
        else
          throw new Error;
      }
      catch(e)
      {
        errno = e;
      }")
   '(:return :throw :break :continue :resume :suspend))
  nil)

(deftest static-analysis/explicitly-terminated-p/nested-throw/2 :notes static-analysis
  (explicitly-terminated-p
   (test-parse "
      foo();
      bar();
      try
      {
        if(baz())
          return 10;
        else
          throw new Error;
      }
      catch(e)
      {
        errno = e;
        throw null;
      }")
   '(:return :throw :break :continue :resume :suspend))
  :throw)

(deftest static-analysis/explicitly-terminated-p/nested-throw/3 :notes static-analysis
  (explicitly-terminated-p
   (test-parse "
      foo();
      bar();
      try
      {
        try
        {
          if(baz())
            return 10;
          else
            throw new Error;
        }
        catch(e)
        {
          errno = e;
          throw null;
        }
      }
      catch(e)
      {
        x = errno;
      }
      baz(x);")
   '(:return :throw :break :continue :resume :suspend))
  nil)

(deftest static-analysis/explicitly-terminated-p/nested-throw/4 :notes static-analysis
  (explicitly-terminated-p
   (test-parse "
      foo();
      bar();
      try
      {
        try
        {
          if(baz())
            return 10;
          else
            throw new Error;
        }
        catch(e)
        {
          errno = e;
          throw null;
        }
      }
      catch(e)
      {
        x = errno;
      }
      break;")
   '(:return :throw :break :continue :resume :suspend))
  :break)
  