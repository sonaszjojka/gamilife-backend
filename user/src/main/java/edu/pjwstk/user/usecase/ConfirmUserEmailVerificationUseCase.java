package edu.pjwstk.user.usecase;

import pl.gamification.api.user.dto.BasicUserInfoApiDto;

import java.util.UUID;

public interface ConfirmUserEmailVerificationUseCase {
    BasicUserInfoApiDto execute(UUID userId);
}
