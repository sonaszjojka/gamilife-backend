package edu.pjwstk.groups.repository;

import edu.pjwstk.groups.entity.Group;
import edu.pjwstk.groups.entity.GroupRequest;
import edu.pjwstk.groups.entity.GroupRequestStatus;
import edu.pjwstk.groups.shared.GroupRequestStatusEnum;

import java.util.UUID;

public interface GroupRequestRepository {

    boolean existsByGroupAndUserIdAndGroupRequestStatus(Group group, UUID userId, GroupRequestStatus groupRequestStatus);

    GroupRequest save(GroupRequest groupRequest);
}
