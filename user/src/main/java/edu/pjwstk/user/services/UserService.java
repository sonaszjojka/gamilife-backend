package edu.pjwstk.user.services;

import edu.pjwstk.user.dto.service.GetUserListItemDto;

import java.util.List;

public interface UserService {
    List<GetUserListItemDto> getAllUsers();
}
