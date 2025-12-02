package edu.pjwstk.user.usecase;

import pl.gamification.api.user.dto.CheckIfUsersEmailIsVerifiedApiDto;

import java.util.UUID;

public interface CheckIfUsersEmailIsVerifiedUseCase {
    CheckIfUsersEmailIsVerifiedApiDto execute(UUID userId);
}
