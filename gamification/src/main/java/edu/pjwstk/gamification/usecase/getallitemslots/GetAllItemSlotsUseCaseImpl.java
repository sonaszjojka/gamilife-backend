package edu.pjwstk.gamification.usecase.getallitemslots;

import edu.pjwstk.gamification.repository.ItemSlotRepository;
import lombok.AllArgsConstructor;
import org.springframework.stereotype.Service;

@Service
@AllArgsConstructor
public class GetAllItemSlotsUseCaseImpl implements GetAllItemSlotsUseCase {

    private final ItemSlotRepository repository;

    @Override
    public GetAllItemSlotsResult executeInternal(GetAllItemSlotsCommand command) {
        return new GetAllItemSlotsResult(
                repository.findAll().stream()
                        .map(itemSlot -> new GetAllItemSlotsResult.ItemSlotDTO(
                                itemSlot.getId(),
                                itemSlot.getName()
                        ))
                        .toList()
        );
    }
}
