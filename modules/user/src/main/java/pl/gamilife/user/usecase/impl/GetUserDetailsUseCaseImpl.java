package pl.gamilife.user.usecase.impl;

import lombok.AllArgsConstructor;
import org.springframework.stereotype.Service;
import pl.gamilife.shared.kernel.exception.domain.UserNotFoundException;
import pl.gamilife.user.domain.User;
import pl.gamilife.user.dto.service.UserDetailsDto;
import pl.gamilife.user.persistence.UserRepository;
import pl.gamilife.user.usecase.GetUserDetailsUseCase;

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
