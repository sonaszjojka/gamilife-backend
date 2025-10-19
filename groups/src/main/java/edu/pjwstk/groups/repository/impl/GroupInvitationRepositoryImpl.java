package edu.pjwstk.groups.repository.impl;

import edu.pjwstk.groups.entity.Group;
import edu.pjwstk.groups.entity.GroupInvitation;
import edu.pjwstk.groups.repository.GroupInvitationRepository;
import edu.pjwstk.groups.repository.jpa.GroupInvitationRepositoryJpa;
import edu.pjwstk.groups.shared.InvitationStatusEnum;
import org.springframework.stereotype.Repository;

import java.util.Optional;
import java.util.UUID;

@Repository
public class GroupInvitationRepositoryImpl implements GroupInvitationRepository {

    private final GroupInvitationRepositoryJpa repositoryJpa;

    public GroupInvitationRepositoryImpl(GroupInvitationRepositoryJpa repositoryJpa) {
        this.repositoryJpa = repositoryJpa;
    }

    @Override
    public Optional<GroupInvitation> findById(UUID groupInvitationId) {
        return repositoryJpa.findById(groupInvitationId);
    }

    @Override
    public void deleteById(UUID groupInvitationId) {
        repositoryJpa.deleteById(groupInvitationId);
    }

    @Override
    public GroupInvitation save(GroupInvitation groupInvitation) {
        return repositoryJpa.save(groupInvitation);
    }

    @Override
    public boolean existsByGroupInvitedAndUserIdAndInvitationStatus(Group group,
                                                                    UUID userId,
                                                                    InvitationStatusEnum invitationStatusEnum) {
        return repositoryJpa.existsByGroupInvitedAndUserIdAndInvitationStatus_InvitationStatusId(group, userId, invitationStatusEnum.getId());
    }

    @Override
    public Optional<GroupInvitation> findByUserIdAndGroupInvitedAndInvitationStatus_InvitationStatusId(UUID userId,
                                                                                                       Group group,
                                                                                                       InvitationStatusEnum invitationStatusEnum) {
        return repositoryJpa.findByUserIdAndGroupInvitedAndInvitationStatus_InvitationStatusId(userId, group, invitationStatusEnum.getId());
    }

}
