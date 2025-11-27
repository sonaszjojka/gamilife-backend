package edu.pjwstk.user.usecase.editusermoney;

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
public class EditUserMoneyUseCaseImpl implements EditUserMoneyUseCase {

    private final UserRepository userRepository;

    @Override
    @Transactional
    public Integer executeInternal(EditUserMoneyCommand cmd) {
        User user = getUser(cmd.userId());

        int newUserMoney = user.getMoney() + cmd.money();
        if (newUserMoney < 0) {
            throw new UserHasNotEnoughMoneyException("User has not enough money");
        }

        userRepository.updateUserMoney(user.getId(), newUserMoney);

        return newUserMoney;
    }

    private User getUser(UUID userId) {
        return userRepository.getUserById(userId)
                .orElseThrow(() -> new UserNotFoundException("User not found"));
    }
}
