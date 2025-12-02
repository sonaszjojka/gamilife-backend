package pl.gamilife.gamification.usecase.getallitemslots;

import pl.gamilife.gamification.repository.ItemSlotRepository;
import lombok.AllArgsConstructor;
import org.springframework.stereotype.Service;

@Service
@AllArgsConstructor
public class GetAllItemSlotsUseCaseImpl implements GetAllItemSlotsUseCase {

    private final ItemSlotRepository repository;

    @Override
    public GetAllItemSlotsResult execute(GetAllItemSlotsCommand cmd) {
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
