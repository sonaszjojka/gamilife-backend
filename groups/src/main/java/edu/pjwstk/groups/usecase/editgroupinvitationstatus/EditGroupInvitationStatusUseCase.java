package edu.pjwstk.groups.usecase.editgroupinvitationstatus;

import jakarta.validation.Valid;

import java.util.UUID;

public interface EditGroupInvitationStatusUseCase {
    EditGroupInvitationStatusResponse execute(UUID groupInvitationId, EditGroupInvitationStatusRequest request);
}
