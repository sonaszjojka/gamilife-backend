package edu.pjwstk.user.persistence;

import edu.pjwstk.user.domain.User;

import java.time.Instant;

public class UserMapper {
    public static UserEntity toEntity(User user) {
        return new UserEntity(
                user.getId(),
                user.getFirstName(),
                user.getLastName(),
                user.getEmail(),
                user.getPassword(),
                user.getUsername(),
                user.getDateOfBirth(),
                user.getLevel(),
                user.getExperience(),
                user.getMoney(),
                user.isSendBudgetReports(),
                user.isProfilePublic(),
                user.isEmailVerified(),
                user.isTutorialCompleted(),
                user.getPasswordChangeDate().getEpochSecond()
        );
    }

    public static User toDomain(UserEntity userEntity) {
        return new User(
                userEntity.getId(),
                userEntity.getFirstName(),
                userEntity.getLastName(),
                userEntity.getEmail(),
                userEntity.getPassword(),
                userEntity.getUsername(),
                userEntity.getDateOfBirth(),
                userEntity.getLevel(),
                userEntity.getExperience(),
                userEntity.getMoney(),
                userEntity.isSendBudgetReports(),
                userEntity.isProfilePublic(),
                userEntity.isEmailVerified(),
                userEntity.isTutorialCompleted(),
                Instant.ofEpochSecond(userEntity.getPasswordChangeDate())
        );
    }
}
