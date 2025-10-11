package edu.pjwstk.user.services;

import edu.pjwstk.user.dto.service.GetUserListItemDto;

import java.util.List;

public interface UserService {
    // For testing purposes only
    @Deprecated
    List<GetUserListItemDto> getAllUsers();
}
