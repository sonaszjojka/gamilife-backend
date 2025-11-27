package edu.pjwstk.user.usecase.levelupuser;

import edu.pjwstk.core.exception.common.domain.UserNotFoundException;
import edu.pjwstk.user.domain.User;
import edu.pjwstk.user.persistence.UserRepository;
import jakarta.transaction.Transactional;
import lombok.AllArgsConstructor;
import org.springframework.stereotype.Service;

import java.util.UUID;

@Service
@AllArgsConstructor
public class LevelUpUserUseCaseImpl implements LevelUpUserUseCase {

    private final UserRepository userRepository;

    @Override
    @Transactional
    public Void executeInternal(LevelUpUserCommand cmd) {
        User user = getUser(cmd.userId());

        user.setLevel(cmd.level());
        userRepository.save(user);

        return null;
    }

    private User getUser(UUID userId) {
        return userRepository.getUserById(userId)
                .orElseThrow(() -> new UserNotFoundException("User not found"));
    }
}
