package edu.pjwstk.user.usecase;

import edu.pjwstk.user.dto.service.UserDetailsDto;

import java.util.UUID;

public interface CompleteOnboardingUseCase {
    UserDetailsDto execute(UUID userId);
}
