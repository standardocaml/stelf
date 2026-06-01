# Desired changes

## History enchanement

History should be stored as lists of entries (each entry being a single request by the user, potentially containing multiple lines).

Also add a temporary entry for the current entry being typed, so as to allow for retrieving it even if the user accidentally navigates away from it.

## Arrow Keys

Arrow keys should work as follows:
1. Left: if not at the beggining of the line, move the cursor to the left. if at the beggining of a line that is not at the start of a request, move the cursor to the end of the previous line. If at the beggining of a request, do nothing.
2. Right: if not at the end of the line, move the cursor to the right. if at the end of a line that is not at the end of a request, move the cursor to the beggining of the next line. If at the end of a request, do nothing.
3. Up: if not at the first line of a request, move the cursor up. if at the first line of a request that is not the first, change the current request (both as displayed and as the current entry in the history) to the previous request, and move the cursor to the end of the first line of that request. In addition, if the current entry is the last entry, store it as the temporary entry.
4. Down: if not at the last line of a request, move the cursor down. if at the last line of a request that is not the last, change the current request (both as displayed and as the current entry in the history) to the next request. If we are at the last entry, and if there is a temporary entry, change the current request to the temporary entry and move the cursor to the end of the last line of that request. If there is no temporary entry, do nothing.

## Prompt enchancements

The input prompt should change as follows:
1. If the current request is empty, the prompt should be `Π∀λ> ` which should be in bold green if the terminal supports it.
2. If the current request is not empty, the prompt should be `...> ` which should be in bold green if the terminal supports it. 
3. Upon the completion of a request, the prompt should change to `=> ` (in bold orange) before output and then after output should change to `Π∀λ> ` again

## Entering requests

There is only one way to end a request. 
It is to have `%.`, followed potentially by whitespace, and then to press the enter key.
Upon completion of a request, the temporary entry should be cleared, the current entry should be added to the history, and the current entry should be cleared.
