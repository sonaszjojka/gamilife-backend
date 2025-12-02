package pl.gamilife.user.usecase.impl;

import pl.gamilife.user.persistence.UserRepository;
import pl.gamilife.user.usecase.UpdateUserEmailUseCase;
import lombok.AllArgsConstructor;
import org.springframework.stereotype.Service;

import java.util.UUID;

@Service
@AllArgsConstructor
public class UpdateUserEmailUseCaseImpl implements UpdateUserEmailUseCase {

    private final UserRepository userRepository;

    @Override
    public void execute(UUID userId, String newEmail) {
        userRepository.updateUserEmail(userId, newEmail);
    }
}
