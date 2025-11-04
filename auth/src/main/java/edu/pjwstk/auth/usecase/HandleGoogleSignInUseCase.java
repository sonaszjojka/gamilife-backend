package edu.pjwstk.auth.usecase;

import edu.pjwstk.auth.usecase.result.GoogleLoginResult;
import edu.pjwstk.auth.usecase.command.HandleGoogleSignInCommand;

public interface HandleGoogleSignInUseCase {
    GoogleLoginResult execute(HandleGoogleSignInCommand handleGoogleSignInCommand);
}
