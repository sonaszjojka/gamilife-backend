package edu.pjwstk.groups.repository.impl;

import edu.pjwstk.groups.entity.InvitationStatus;
import edu.pjwstk.groups.repository.InvitationStatusRepository;
import edu.pjwstk.groups.repository.jpa.InvitationStatusRepositoryJpa;
import org.springframework.stereotype.Repository;

import java.util.Optional;

@Repository
public class InvitationStatusRepositoryImpl implements InvitationStatusRepository {

    private final InvitationStatusRepositoryJpa repositoryJpa;

    public InvitationStatusRepositoryImpl(InvitationStatusRepositoryJpa repositoryJpa) {
        this.repositoryJpa = repositoryJpa;
    }

    @Override
    public Optional<InvitationStatus> findById(Integer id) {
        return repositoryJpa.findById(id);
    }
}
