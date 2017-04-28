Voici les instructions pour utiliser Git sur votre ordinateur

### Installer git

1. Créer un compte [GitHub](www.github.com). 
2. Suivre (Watch) le répertoire LaboScript
2. Installer [git](https://git-scm.com/) sur votre ordinateur.
3. À chaque fois que vous voulez utiliser une fonctionalité de Git sur votre ordinateur vous pouvez ouvrir le GUI de git en ouvrant un menu contextuel et en cliquant sur Git GUI here

![Ouvrir le GUI de Git](https://github.com/RichardLaBrie/LaboScript/blob/master/Images/Inst1.png)

### Clone le répertoire GRIL script 
Après avoir initialisé votre compte GitHub vous devez cloner le répertoire GRIL script sur votre ordinateur. 

1. Ouvrir le GUI de git dans un dossier (voir étape 3) 
2. Cliquer sur **Clone Existing Repository** 
3. Dans Source Location copier: https://github.com/RichardLaBrie/LaboScript.git
4. Dans Target Directory identifier un fichier pour copier le répertoire (le fichier ne doit pas déja exister)
5. Cliquer sur **Clone**

![Ouvrir le GUI de Git](https://github.com/RichardLaBrie/LaboScript/blob/master/Images/Inst2.png)


Après avoir cloné le répertoire il devrait être accessible sur votre ordinateur

### Comment travailler avec Git

Avant de travailler sur un fichier Git il faut toujours faire un *"pull"* afin d'avoir la version la plus récente et déviter les conflits:

1. Aller dans le répertoire sur votre ordinateur et ouvrir le GUI Git (voir étape 3)
2. Cliquer sur Remote/fetch from /origin / close
2. Cliquer sur Merge/ local Merge (ou ctrl-M)/ merge

Maintenant vous pouvez travailler dans le dossier normalement, vous pouvez ajouter, modifier et effacer des fichiers. Quand vous avez terminé vos modifications il faut mettre à jour (*"push"*) le répertoire en ligne (sur GitHub). 

1. Aller dans le répertoire sur votre ordinateur et ouvrir le GUI Git (voir étape 3)
2. Vous allez voir les fichier que vous avez modifié dans la section *"Unstaged Changes"*

![Ouvrir le GUI de Git](https://github.com/RichardLaBrie/LaboScript/blob/master/Images/Inst3.1.png)


3. Vous devez mettre les changements que vous voulez mettre à jour dans le section *"Staged Changes"* sois en sélectionnant le fichier puis en appuyant sur Ctrl-T ou en appuyant sur Ctrl-I pour *"Stager"* tous les fichiers
4. Par la suite il faut ajouter un message expliquant ce que nous vous avez changé dans la section *"commit message"* puis cliquer sur *"Commit"* 
5. Finalement il faut appuyer sur *"Push"* pour finaliser la mise à jour
![Ouvrir le GUI de Git](https://github.com/RichardLaBrie/LaboScript/blob/master/Images/Inst3.png)

## Aidez-moi j'ai un problème!

Si vous avez un problème ou une question sur un script vous pouvez les reporter directement sur le repertoire GitHub dans la section Issues.


