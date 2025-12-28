package pl.gamilife.app.service;

import pl.gamilife.app.dto.user.UserDetailsResponse;

import java.util.UUID;

public interface UserDetailsService {

    UserDetailsResponse getUserDetails(UUID currentUserId, UUID targetUserId);

}
