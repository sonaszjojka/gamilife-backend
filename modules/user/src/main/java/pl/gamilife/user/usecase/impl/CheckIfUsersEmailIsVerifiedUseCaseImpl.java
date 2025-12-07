package pl.gamilife.user.usecase.impl;

import lombok.AllArgsConstructor;
import org.springframework.stereotype.Service;
import pl.gamilife.api.user.dto.CheckIfUsersEmailIsVerifiedApiDto;
import pl.gamilife.shared.kernel.exception.domain.UserNotFoundException;
import pl.gamilife.user.domain.User;
import pl.gamilife.user.persistence.UserRepository;
import pl.gamilife.user.usecase.CheckIfUsersEmailIsVerifiedUseCase;

import java.util.UUID;

@Service
@AllArgsConstructor
public class CheckIfUsersEmailIsVerifiedUseCaseImpl implements CheckIfUsersEmailIsVerifiedUseCase {

    private UserRepository userRepository;

    @Override
    public CheckIfUsersEmailIsVerifiedApiDto execute(UUID userId) {
        User user = userRepository.getUserById(userId)
                .orElseThrow(() -> new UserNotFoundException("User not found"));

        return new CheckIfUsersEmailIsVerifiedApiDto(
                user.isEmailVerified(),
                user.getEmail()
        );
    }
}
