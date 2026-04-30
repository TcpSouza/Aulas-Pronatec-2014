select aluno!código,aluno!aluno,curso!código,curso!curso,matricula!Códigoa,matricula!códigoc FROM 
ALUNO,CURSO,MATRICULA where ((aluno![CÓDIGO]=matricula![CÓDIGOA]) and                                
(curso![CÓDIGO]=matricula![CÓDIGOC]));
