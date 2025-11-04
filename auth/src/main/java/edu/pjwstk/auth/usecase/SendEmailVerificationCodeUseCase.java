package edu.pjwstk.auth.usecase;

import java.util.UUID;

@Deprecated
public interface SendEmailVerificationCodeUseCase {
    void execute(UUID userId);
}
