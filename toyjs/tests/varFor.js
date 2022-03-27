//Taken from a eval in test262.
// Caused a infinite loop

for (var i = 0; i < 2; ++i) { 
    if (i) { 
        try { 
            throw null; 
        } catch (e) { 
            break; 
        } 
    } 
    'bad completion'; 
}
