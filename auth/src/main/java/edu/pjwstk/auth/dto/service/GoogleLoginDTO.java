package edu.pjwstk.auth.dto.service;

import lombok.Getter;

import java.util.UUID;

@Getter
public class GoogleLoginDTO {
    private final LoginType loginType;
    private final String accessToken;
    private final String refreshToken;
    private final String providerId;
    private final String providerName;
    private final UUID userId;

    public enum LoginType {
        NEW_USER,
        EXISTING_USER,
        POSSIBLE_LINK
    }

    public GoogleLoginDTO(LoginType loginType, String accessToken, String refreshToken) {
        if (loginType != LoginType.NEW_USER && loginType != LoginType.EXISTING_USER) {
            throw new IllegalStateException("loginType must be either NEW_USER or EXISTING_USER to use this constructor");
        }

        this.loginType = loginType;
        this.accessToken = accessToken;
        this.refreshToken = refreshToken;
        this.providerId = null;
        this.providerName = null;
        this.userId = null;
    }

    public GoogleLoginDTO(LoginType loginType, String providerName, String providerId, UUID userId) {
        if (loginType != LoginType.POSSIBLE_LINK) {
            throw new IllegalStateException("loginType must be POSSIBLE_LINK to use this constructor");
        }

        this.loginType = loginType;
        this.accessToken = null;
        this.refreshToken = null;
        this.providerId = providerId;
        this.providerName = providerName;
        this.userId = userId;
    }

}
