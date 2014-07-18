#include<cstdio>
int main() {
  const char*s="#include<cstdio>\nint main() {\n  const char*s=\"%s\";\n  char b[2048];\n  int i,j=0;\n  // escape the string\n  for(i=0;s[i]!=0;i++)\n    if(s[i]=='\"'){b[j++]='\\\\';b[j++]='\"';}\n    else if(s[i]=='\\n'){b[j++]='\\\\';b[j++]='n';}\n    else if(s[i]=='\\\\'){b[j++]='\\\\';b[j++]='\\\\';}\n    else b[j++]=s[i];\n  b[j]=0;\n  printf(s,b);\n  return 0;\n}\n";
  char b[2048];
  int i,j=0;
  // escape the string
  for(i=0;s[i]!=0;i++)
    if(s[i]=='"'){b[j++]='\\';b[j++]='"';}
    else if(s[i]=='\n'){b[j++]='\\';b[j++]='n';}
    else if(s[i]=='\\'){b[j++]='\\';b[j++]='\\';}
    else b[j++]=s[i];
  b[j]=0;
  printf(s,b);
  return 0;
}
