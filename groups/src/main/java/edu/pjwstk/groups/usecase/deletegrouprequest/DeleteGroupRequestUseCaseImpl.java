package edu.pjwstk.groups.usecase.deletegrouprequest;

import edu.pjwstk.groups.exception.GroupRequestNotFoundException;
import edu.pjwstk.groups.repository.GroupRequestRepository;
import edu.pjwstk.groups.repository.jpa.GroupRequestRepositoryJpa;
import org.springframework.stereotype.Service;
import org.springframework.transaction.annotation.Transactional;

import java.util.UUID;

@Service
public class DeleteGroupRequestUseCaseImpl implements DeleteGroupRequestUseCase {

    private final GroupRequestRepository groupRequestRepository;

    public DeleteGroupRequestUseCaseImpl(GroupRequestRepository groupRequestRepository) {
        this.groupRequestRepository = groupRequestRepository;
    }

    @Override
    @Transactional
    public void execute(UUID groupRequestId) {
        groupRequestRepository.findById(groupRequestId)
                .orElseThrow(() -> new GroupRequestNotFoundException("Group request with id:" + groupRequestId + " not found!"));

        groupRequestRepository.deleteById(groupRequestId);
    }
}
