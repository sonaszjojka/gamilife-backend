package edu.pjwstk.groups.usecase.deletegroupinvitation;

import java.util.UUID;

public interface DeleteGroupInvitationById {
    void execute(UUID groupInvitationId);
}
