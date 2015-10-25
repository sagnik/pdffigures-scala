package edu.ist.psu.sagnik.research.extractor.regex;

/**
 * Created by szr163 on 10/1/15.
 */
import java.util.regex.Pattern;
import java.util.regex.Matcher;

public class RegexTestHarness {


    public int match_figure_start(String inpline){
        String inpPatternfig = "^[\\s]*(Figure|Fig\\.|FIGURE|FIG\\.|Fig\\s|FIG\\s|Table|TABLE)[\\s]*";
        String totString=inpline;
        Pattern pattern =
                Pattern.compile(inpPatternfig);

        Matcher matcher =
                pattern.matcher(totString);
        while (matcher.find()) {
            return (matcher.end());
        }
        return 0;

    }

    public int indexAfterCaptionID(String inpline){
        String this_input=inpline;
        int idbigindex=match_figure_start(this_input);
        int idnumendindex=0;
        int firstcharafteridandpunct=0;

        if(idbigindex!=0){
            idnumendindex=match_id_end_numeric(this_input.substring(idbigindex));
            if (idnumendindex==0)
                return 0;
            else {
                firstcharafteridandpunct=remove_punct(this_input.substring(idbigindex+idnumendindex));
                return (idbigindex+idnumendindex+firstcharafteridandpunct);
            }
        }
        return 0;
    }

    private int remove_punct(String inpLine) {
        String inpPattern="[\\s]*(\\p{Alpha}[\\s][\\s]*|\\p{Alpha}\\p{Alpha})";
        String totString=inpLine;
        Pattern pattern =
                Pattern.compile(inpPattern);

        Matcher matcher =
                pattern.matcher(totString);
        while (matcher.find()) {
            return (matcher.end()-2);
        }
        return 0;
    }

    private int match_id_end_numeric(String inpline) {
        String inpPatternfigidnumeric = "[\\s]*[\\d\\.]*[\\d][\\d]*[\\.]*[\\s]*";
        String totString=inpline;

        Pattern pattern =
                Pattern.compile(inpPatternfigidnumeric);

        Matcher matcher =
                pattern.matcher(totString);

        boolean found = false;
        if (matcher.find()) {
            found=true;
        }
        if(!found)
            return 0;
        else
            return(matcher.end());

    }


    public boolean CheckforMention(String figid, String inpline){
        String inpPattern = "(figure|fig\\.)(.){0,15}?"+figid+"[\\D]";//"[\\s*|[\\s]*\\.|(|)]";
        String totString=inpline.toLowerCase()+" ";

        Pattern pattern =
                Pattern.compile(inpPattern);

        Matcher matcher =
                pattern.matcher(totString);

        boolean found = false;
        while (matcher.find()) {
            found = true;
        }
        if(!found){
            return false;
        }
        else
            return true;

    }

    public static void main(String[] args){
        String id="2.3.4.1";
        String abc="  Fig. 2.3.4.1 Shows the values of pKTS9 for the alcohol-mediated. ";
        String t1="Fig. 1 A plot of the solubility, as log L, for alkan-1-ol vapours in";
        String t2="in terms of eqn. (7) generally worked well, as shown in Fig. 2";
        t2="protection (Fig. 2) and as pest deterrents in both domestic use";
        t1="omitted for clarity), Fig. 2, whereby extraction of the com";
        //t2=" Fig. 1: The ";
        t2= " Fig. 3.1.2.3. (a)): A this figure amounts to almost 10 kcal mol21";
        abc="Figure 1: Process Attributes:fontdiff: 1";
        abc="Fig. 3). If relativistic effects are negligible (v0 ≪ c), the value:";
        abc=" FIGURE 1: a RELATIVE CONCENTRATION PROFILES FOR CO";
        abc=" hello, and we want figure 1a, 2b and 3c to match here";
        abc="The citation context shown in this example is the same as that shown in Figure 2.";
        abc="Fig.2UnitcellparametersasafunctionoftemperatureforC60Ar@C60.";
        abc="Figure11:hellohowryou";
        abc="  Fig. 2.3.4.1 Shows the values of pKTS9 for the alcohol-mediated. ";
        abc="hurumdurumkurum";
        int figuretextstart=new RegexTestHarness().indexAfterCaptionID(abc);

        System.out.println(abc.substring(figuretextstart));
        //System.out.println(t2.toLowerCase());
        //System.out.println(new RegexTestHarness().indexAfterCaptionID(abc)+" "+abc.substring(new RegexTestHarness().indexAfterCaptionID(abc)));
        //System.out.println(new RegexTestHarness().CheckforMention("1", abc));
        //System.out.println(abc.substring(new RegexTestHarness().punctAfterCaptionID(abc)));
        //System.out.println(new RegexTestHarness().CheckforMention("2",abc.toLowerCase()));
        //System.out.println(""+t1.trim().endsWith("."));
        //String inpPattern="(^Figure|^Fig[\\.][\\s]*"+id;
    }



}