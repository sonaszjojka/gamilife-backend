package edu.pjwstk.groups.repository.impl;

import edu.pjwstk.groups.entity.GroupInvitation;
import edu.pjwstk.groups.repository.GroupInvitationRepository;
import edu.pjwstk.groups.repository.jpa.GroupInvitationRepositoryJpa;
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
}
