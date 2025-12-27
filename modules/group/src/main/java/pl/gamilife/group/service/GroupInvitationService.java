package pl.gamilife.group.service;

import pl.gamilife.group.model.Group;
import pl.gamilife.group.model.GroupInvitation;

import java.util.UUID;

public interface GroupInvitationService {
    GroupInvitation createGroupInvitation(Group group, UUID userId);

}


