package edu.pjwstk.auth.persistence.mapper;

import edu.pjwstk.auth.dto.service.UserOAuthProvider;
import edu.pjwstk.auth.persistence.entity.UserOAuthProviderEntity;

public class UserOAuthProviderMapper {

    public static UserOAuthProviderEntity toEntity(UserOAuthProvider userOAuthProvider) {
        return new UserOAuthProviderEntity(
                userOAuthProvider.id(),
                userOAuthProvider.userId(),
                userOAuthProvider.provider(),
                userOAuthProvider.providerId()
        );
    }

    public static UserOAuthProvider toDomain(UserOAuthProviderEntity userOAuthProviderEntity) {
        return new UserOAuthProvider(
                userOAuthProviderEntity.getId(),
                userOAuthProviderEntity.getUserId(),
                userOAuthProviderEntity.getProvider(),
                userOAuthProviderEntity.getProviderId()
        );
    }

}
