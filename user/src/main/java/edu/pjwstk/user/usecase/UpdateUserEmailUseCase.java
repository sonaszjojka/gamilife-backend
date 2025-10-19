package edu.pjwstk.user.usecase;

import java.util.UUID;

public interface UpdateUserEmailUseCase {
    void execute(UUID userId, String newEmail);
}
