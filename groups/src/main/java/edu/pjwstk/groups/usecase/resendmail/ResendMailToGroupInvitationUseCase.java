package edu.pjwstk.groups.usecase.resendmail;

import java.util.UUID;

public interface ResendMailToGroupInvitationUseCase {
    void execute(UUID groupId, UUID groupInvitationId);
}
