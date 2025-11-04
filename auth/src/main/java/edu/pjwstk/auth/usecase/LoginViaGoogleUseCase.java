package edu.pjwstk.auth.usecase;

import edu.pjwstk.auth.usecase.result.GoogleLoginResult;

import java.util.UUID;

@Deprecated
public interface LoginViaGoogleUseCase {
    GoogleLoginResult execute(UUID userId, String googleEmail);
}
