package edu.pjwstk.auth.usecase;

import edu.pjwstk.auth.dto.service.GoogleLoginDTO;

import java.util.UUID;

public interface LoginViaGoogleUseCase {
    GoogleLoginDTO execute(UUID userId, String googleEmail);
}
