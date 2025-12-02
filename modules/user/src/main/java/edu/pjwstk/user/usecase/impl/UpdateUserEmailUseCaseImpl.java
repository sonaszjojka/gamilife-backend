package edu.pjwstk.user.usecase.impl;

import edu.pjwstk.user.persistence.UserRepository;
import edu.pjwstk.user.usecase.UpdateUserEmailUseCase;
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
