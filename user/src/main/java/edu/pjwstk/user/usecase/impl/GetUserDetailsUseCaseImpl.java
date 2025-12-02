package edu.pjwstk.user.usecase.impl;

import pl.gamilife.infrastructure.core.exception.common.domain.UserNotFoundException;
import edu.pjwstk.user.domain.User;
import edu.pjwstk.user.dto.service.UserDetailsDto;
import edu.pjwstk.user.persistence.UserRepository;
import edu.pjwstk.user.usecase.GetUserDetailsUseCase;
import lombok.AllArgsConstructor;
import org.springframework.stereotype.Service;

import java.util.UUID;

@Service
@AllArgsConstructor
public class GetUserDetailsUseCaseImpl implements GetUserDetailsUseCase {

    private final UserRepository userRepository;

    @Override
    public UserDetailsDto execute(UUID userId) {
        User user = userRepository
                .getUserById(userId)
                .orElseThrow(() -> new UserNotFoundException("User not found"));

        return new UserDetailsDto(
                user.getId(),
                user.getFirstName(),
                user.getLastName(),
                user.getEmail(),
                user.getUsername(),
                user.getDateOfBirth(),
                user.getExperience(),
                user.getMoney(),
                user.isSendBudgetReports(),
                user.isProfilePublic(),
                user.isEmailVerified(),
                user.isTutorialCompleted()
        );
    }
}
