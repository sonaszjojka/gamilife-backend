package edu.pjwstk.common.userApi.dto;

import java.util.UUID;

public record BasicUserInfoApiDto(UUID userId, String email, String username) {
}
