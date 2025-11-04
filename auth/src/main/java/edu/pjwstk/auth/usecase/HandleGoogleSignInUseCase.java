package edu.pjwstk.auth.usecase;

import edu.pjwstk.auth.usecase.command.HandleGoogleSignInCommand;
import edu.pjwstk.auth.usecase.result.GoogleLoginResult;

public interface HandleGoogleSignInUseCase {
    GoogleLoginResult execute(HandleGoogleSignInCommand handleGoogleSignInCommand);
}
