package edu.pjwstk.user.usecase;

import java.util.UUID;

public interface ResetUserPasswordUseCase {
    void execute(UUID userId, String hashedNewPassword);
}
