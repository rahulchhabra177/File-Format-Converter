exception UnevenFields;
exception Double_quotes;
exception end_of_line;
exception emptyInputFile;

fun Uneven_field()=
  raise UnevenFields;

fun Double_quotes_error()=
  raise Double_quotes;

fun EOL()=
  raise end_of_line;

fun emptyInput()=
  raise emptyInputFile;
(*
First the input is read from instream character by character 
I have defined 4 states numbered 1 ,2 ,3 and 4 each describing the current stage of input reading done
State 1 => Even number of double quotes has been encountered till now and currecnt char is not a first char of any field. 
State 2 => Odd number of double quotes have been encountered till now
State 3 => Current char is going to be the first character of any field
State 4 => Currently ecploring a field which was not started by a (") double quote.

*)

fun convertDelimiters(infile,delim1,outfile,delim2) =
   let
      val is= TextIO.openIn infile  
      val os=TextIO.openAppend outfile
      val a=String.str(delim1)
      val b=String.str(delim2)
      (*
      Input file is read using instream "is" and output file is modified using outstream "os"
      a is the string format of delim1 and b is string format of delim2

      *)
  (*loop is a helper function with following parameters:
    is => instream;
    even => a boolean variable which is true if number of encountered double quote is even else it is false
    state => it defines the state which was defined above
    line_num => represents the current line number
    querynum => column number of current entry
    entry => number of entries in the firt line
    last_char =>previous encountered character   

    *)
    fun loop (is,even,state,line_num,querynum,entry,last_char) =
      (*k is the current character in string format*)
      let val k=TextIO.inputN(is,1) 

        in

    if (k="") then
      (*This means that there are no more charavters left in input file*) 
        if line_num=1 andalso querynum=0 andalso last_char="" then emptyInput() 
        else if (state=2 orelse not even) then Double_quotes_error() handle Double_quotes=>print"Double quotes not in Correct Format\n"
        else if not(last_char="\n") then EOL() handle end_of_line =>print"Last entry not followed by newline(LF)\n"  
        (*cheking whether the last record is ended by newline*)
        else TextIO.output(os,"")
    (*If we are at state = 1 that means even number of " encounterd till now so getting one another quote will change the state to state=2
      If we get the delimiter 1 then this means a new field has been encountered so state is changes to state=3 with neccesary outputs
      If we get end of line this means a new record is going to be encountered so first we chwck whther current number of fileds in row are same as that in line 1
      If we get any other character then we just print it in the output file
    *)
    else if (state=1) then 
            if (k="\"") then (TextIO.output(os,"\"");loop(is,not even,2,line_num,querynum,entry,k))
            else if k=a then (TextIO.output(os,b);loop(is,even,3,line_num,querynum+1,entry,b))
            else if k="\n" then 
                    if (line_num=1) then (TextIO.output(os,k);loop(is,even,3,line_num+1,0,querynum,k))
                    else 
                          if  (entry=querynum) then (TextIO.output(os,k);loop(is,even,3,line_num+1,0,entry,k))
                          else (TextIO.output(os,k);Uneven_field()handle UnevenFields => print (concat(["Expected : ",Int.toString(entry+1)," fields , Present: ",Int.toString(querynum+1)," fields on Line ",Int.toString(line_num),"\n"])))

            else (TextIO.output(os,k);loop(is,even,state,line_num,querynum,entry,k))

    (*If we are at state =2 that means odd number of " encounterd till now so getting one another quote will change the state to state=1 
      If we get any other character then we just print it in the output file
    *)
    else if (state=2) then 
            if (k="\"") then (TextIO.output(os,k);loop(is,not even,1,line_num,querynum,entry,k))
            else (TextIO.output(os,k);loop(is,even,state,line_num,querynum,entry,k))


    (*If we are at state =3 that means we are at starting of some field so getting one quote will change the state to state=1
      If we get end of line this means we have last entry to be empty and a new record is going to be encountered so we add "" for that empty field and first we chwck whether current number of fileds in row are same as that in line 1
      If we get any other character then we just print it in the output file
    *)
    else if state=3 then 
            if k=a then (TextIO.output(os,"\"");TextIO.output(os,"\"");TextIO.output(os,b);loop(is,even,state,line_num,querynum+1,entry,b))
            else if k="\"" then (TextIO.output(os,k);loop(is,not even,2,line_num,querynum,entry,k))
            else if k="\n" then 
                    if (line_num=1) then (TextIO.output(os,"\"");TextIO.output(os,"\"");TextIO.output(os,k);loop(is,even,3,line_num+1,0,querynum,k))
                    else 
                            if  (entry=querynum) then (TextIO.output(os,"\"");TextIO.output(os,"\"");TextIO.output(os,k);loop(is,even,3,line_num+1,0,entry,k))
                            else (TextIO.output(os,k);Uneven_field() handle UnevenFields => print (concat(["Expected : ",Int.toString(entry+1)," fields , Present: ",Int.toString(querynum+1)," fields on Line ",Int.toString(line_num),"\n"])))

            else (TextIO.output(os,"\"");TextIO.output(os,k);loop(is,even,4,line_num,querynum,entry,k))
    (*If we are at state = 4 that means we are in a field which was not started by a (") in the input file so we had put one double quote there and now if we get a delim1 this means current field is completed so we add ending quote and change the state to 3
      If we get end of line this means a new record is going to be encountered so we add the ending double quote and first we check whther current number of fileds in row are same as that in line 1
      If we get any other character then we just print it in the output file
    *)          
    else  
            if k=a then (TextIO.output(os,"\"");TextIO.output(os,b);loop(is,even,3,line_num,querynum+1,entry,b))
            else if k="\n" then 
                            if (line_num=1) then (TextIO.output(os,"\"");TextIO.output(os,k);loop(is,even,3,line_num+1,0,querynum,k))
                            else 
                                  if  (entry=querynum) then (TextIO.output(os,"\"");TextIO.output(os,k);loop(is,even,3,line_num+1,0,entry,k))
                                  else (TextIO.output(os,k);Uneven_field()  handle UnevenFields => print (concat(["Expected : ",Int.toString(entry+1)," fields , Present: ",Int.toString(querynum+1)," fields on Line ",Int.toString(line_num),"\n"])))

            else if (k="\"") then (TextIO.output(os,k);loop(is,not even,state,line_num,querynum,entry,k))
            else (TextIO.output(os,k);loop(is,even,state,line_num,querynum,entry,k))       
                        
                        end
                        in 
                          (*since initially number of encountered double quotes is zero which is even so we give enen=true
                          initial state will be 3 as its starting of a new field
                          line num is 1
                          query num I have takein to be zero for my convinience 
                          last char is empty string and entry is zero for reference

                          *)
           loop(is,true,3,1,0,0,"") before TextIO.closeIn is before TextIO.closeOut os

  end;

fun csv2tsv(infilename, outfilename)=
  convertDelimiters(infilename,#",",outfilename,#"\t");

fun tsv2csv(infilename, outfilename)=
  convertDelimiters(infilename,#"\t",outfilename,#",");

