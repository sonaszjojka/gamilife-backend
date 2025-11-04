package edu.pjwstk.auth.usecase.googlesignin;

public interface HandleGoogleSignInUseCase {
    GoogleLoginResult execute(HandleGoogleSignInCommand handleGoogleSignInCommand);
}
