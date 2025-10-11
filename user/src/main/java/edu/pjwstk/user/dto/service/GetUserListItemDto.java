package edu.pjwstk.user.dto.service;

public record GetUserListItemDto(
        String email,
        String password,
        String username
) {
}
