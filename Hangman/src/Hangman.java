import java.util.*;
public class Hangman {
	static Scanner in=new Scanner(System.in);
	public static void main(String args[]) throws Exception {
		Thread.sleep(2500);
		System.out.println("Hello gamer, welcome to this silly and simple game\n");
		Thread.sleep(2500);
		System.out.println("But before that,please entter your name: ");
		String name=in.nextLine();
		System.out.println("\nKept you waiting,huh!\nLets start the game\n");
		play(name);
		
	}
	static void play(String name) {
		char alpha[]=new char[26];
		int k=0;
		boolean win=false;
		Initialize a = new Initialize();
		for(int i=0;i<a.l;i++) {a.ans[i]='_';}
		System.out.println("Your question is : ");
		display(a.ans);
		String wc=a.w;
		for(int x=0;x<a.count;x++) {
			System.out.println("Enter your guess : ");
			char g = in.next().charAt(0);
			if(isPresent(g,a.ans)) {
				System.out.println("The letter "+g+" is already filled\nCheck with another letter");
			}
			else if(wc.contains(g+"")) {
				checkandfill(g,wc,a.ans);
				System.out.println("Well done : ");
				alpha[k]=g; k++;
				a.count = (a.count>0)?a.count--:0;
			}
			else {
				System.out.println("The letter "+g+" is not present in the word\nPlease try again");
			}
			display(a.ans);
			if(!isPresent('_',a.ans)) {win=true; break;}
		}
		if(isPresent('_',a.ans)) {System.out.println("\nSorry, you lost the game");}
		if(win) {System.out.println("Congratulations "+name+" won the game");}
	}
	static boolean isPresent(char a,char b[]) {
		for(int i=0;i<b.length;i++) {
			if(a==b[i]) {return true;}
		}
		return false;
	}
	static void display(char c[]) {
		for(int i=0;i<c.length;i++) {System.out.print(c[i]+" ");}
		System.out.println();
	}
	static void checkandfill(char c,String s,char[] a) {
		for(int i=0;i<s.length();i++) {
			if(c==s.charAt(i)) {a[i]=c;}
		}
	}
}
class Initialize{
	public static String Random(String[] a) {
	    int rnd = new Random().nextInt(a.length);
	    return a[rnd];
	}
	private String qs[]= {"cse","medicine","axis","science","intel","platform"};
	String w=Random(qs);
	int l=w.length();
	char ans[]=new char[l];
	int count=5;
	
}
