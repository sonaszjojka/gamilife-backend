package edu.pjwstk.auth.usecase;

import java.util.UUID;

public interface SendEmailVerificationCodeUseCase {
    void execute(UUID userId);
}
