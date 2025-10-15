package edu.pjwstk.groups.repository.impl;

import edu.pjwstk.groups.entity.Group;
import edu.pjwstk.groups.entity.GroupRequest;
import edu.pjwstk.groups.entity.GroupRequestStatus;
import edu.pjwstk.groups.repository.GroupRequestRepository;
import edu.pjwstk.groups.repository.jpa.GroupRequestRepositoryJpa;
import edu.pjwstk.groups.shared.GroupRequestStatusEnum;
import org.springframework.stereotype.Repository;

import java.util.UUID;

@Repository
public class GroupRequestRepositoryImpl implements GroupRequestRepository {

    private final GroupRequestRepositoryJpa repositoryJpa;

    public GroupRequestRepositoryImpl(GroupRequestRepositoryJpa repositoryJpa) {
        this.repositoryJpa = repositoryJpa;
    }


    @Override
    public boolean existsByGroupAndUserIdAndGroupRequestStatus(Group group, UUID userId, GroupRequestStatus groupRequestStatus) {
        return repositoryJpa.existsByGroupRequestedAndUserIdAndGroupRequestStatus(group, userId, groupRequestStatus);
    }

    @Override
    public GroupRequest save(GroupRequest groupRequest) {
        return repositoryJpa.save(groupRequest);
    }
}
