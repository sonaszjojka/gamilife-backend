package edu.pjwstk.user.usecase.subtractomoneyfromuser;

import edu.pjwstk.core.exception.common.domain.UserHasNotEnoughMoneyException;
import edu.pjwstk.core.exception.common.domain.UserNotFoundException;
import edu.pjwstk.user.domain.User;
import edu.pjwstk.user.persistence.UserRepository;
import jakarta.transaction.Transactional;
import lombok.AllArgsConstructor;
import org.springframework.stereotype.Service;

import java.util.UUID;

@Service
@AllArgsConstructor
public class SubtractMoneyFromUserUseCaseImpl implements SubtractMoneyFromUserUseCase {

    private final UserRepository userRepository;

    @Override
    @Transactional
    public Void executeInternal(SubtractMoneyFromUserCommand cmd) {
        User user = getUser(cmd.userId());

        int newUserMoney = user.getMoney() - cmd.money();
        if (newUserMoney < 0) {
            throw new UserHasNotEnoughMoneyException("User has not enough money");
        }

        userRepository.updateUserMoney(user.getId(), user.getMoney() - cmd.money());

        return null;
    }

    private User getUser(UUID userId) {
        return userRepository.getUserById(userId)
                .orElseThrow(() -> new UserNotFoundException("User not found"));
    }
}
