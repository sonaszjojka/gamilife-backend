package edu.pjwstk.groups.usecase.creategroupinvitation;

import java.util.UUID;

public interface CreateGroupInvitationUseCase {
    CreateGroupInvitationResponse execute(UUID groupId, CreateGroupInvitationRequest request);
}
