package edu.pjwstk.auth.dto.service;

public record OAuthCodeDto(
    String code,
    String codeVerifier
) {
}
