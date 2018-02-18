BEGIN {
    st = 0
}

# Currently there is no need to add any special management for
# the end of the package since the previous declarations are
# sent to the output. However, this special management is added
# to handle minor future changes located at the end of the source
# file.

END {
    End_Pkg = "end Ada.Exceptions;"

    if (Last_Output_Line != End_Pkg) {
       print "end Ada.Exceptions;"
    }
}

st == 1 && /^ +procedure Rcheck_PE_Finalize_Raised_Exception/ {
   Saved_Line = $0

   # Force reading the next line (needed to know if this is the
   # subprogram declaration or its body)
   getline

   # Handle subprogram declaration
   if (substr($0,length($0),1) == ";") {
      print Saved_Line
      print $0
      next

   # Enable stage 2 (search for the end of this concrete subprogram)
   } else {
      st = 2
   }
}

# Enable stage 1: procedure declarations sent to the output
st == 0 && /^ +procedure Rcheck_/ {
    st = 1
}

# Enable stage 1: message declarations sent to the output
st == 0 && /^ +Rmsg_/ {
    st = 1
}

###########
# Stage 1 #
###########

# Remove C-style null termination to the messages
st == 1 && / +& NUL;/ {
   sub (/ +& NUL;/,";",$0)
}

# Update convention of pragma Export
st == 1 && /Export \(C,/ {
   sub (/Export \(C,/,"Export (@TARGETLANG@,",$0)
}

# Remove 'Address to msg arguments
st == 1 && /'Address/ {
   sub (/'Address/,"",$0)
}

# End of stage 1
st == 1 && /^ *------------/ {
    st = 0
}

# Mirror this line
st == 1 {
  print $0
  Last_Output_Line = $0
}

###########
# Stage 2 #
###########

# Special management for this subprogram
st == 2 && /^ +end Rcheck_PE_Finalize_Raised_Exception/ {
    st = 0
    print "   procedure Rcheck_PE_Finalize_Raised_Exception"
    print "     (File : System.Address; Line : Integer) is"
    print "   begin"
    print "      Raise_Program_Error_Msg (File, Line, Rmsg_23);"
    print "   end Rcheck_PE_Finalize_Raised_Exception;"
    print ""
}

